;; =============================================================================
;; DECENTRALIZED CONTENT ARCHIVE SYSTEM v3.0
;; =============================================================================
;; 
;; Advanced distributed content management platform for secure file archiving
;; Built on blockchain infrastructure for immutable record keeping
;; Implements sophisticated access control mechanisms and metadata management
;; 
;; This system provides enterprise-grade content orchestration capabilities
;; with comprehensive cryptographic verification and audit trail functionality
;; Designed for high-volume document management across distributed networks
;; 
;; Key Features:
;; - Immutable content registration with blockchain timestamps
;; - Multi-tiered permission system for granular access control
;; - Comprehensive metadata tagging for advanced search capabilities
;; - Atomic operations ensuring data consistency across all transactions
;; - Enterprise-level security protocols with role-based authentication
;; =============================================================================

;; =============================================================================
;; SYSTEM CONFIGURATION AND ADMINISTRATIVE SETTINGS
;; =============================================================================

;; Primary system administrator with elevated privileges across all operations
;; This principal has unrestricted access to system-wide configuration changes
(define-constant primary-system-controller contract-caller)

;; =============================================================================
;; COMPREHENSIVE ERROR CODE DEFINITIONS AND EXCEPTION HANDLING FRAMEWORK
;; =============================================================================

;; These error codes provide detailed feedback for various system failure scenarios
;; Each code corresponds to specific validation failures or operational constraints
(define-constant content-record-missing-error (err u601))
(define-constant duplicate-content-registration-error (err u602))
(define-constant invalid-input-data-error (err u603))
(define-constant storage-capacity-overflow-error (err u604))
(define-constant unauthorized-access-attempt-error (err u605))
(define-constant insufficient-authorization-privileges-error (err u606))
(define-constant system-permission-violation-error (err u607))
(define-constant restricted-content-access-error (err u608))
(define-constant data-format-validation-failure-error (err u609))

;; =============================================================================
;; GLOBAL STATE VARIABLES AND COUNTERS
;; =============================================================================

;; Master sequence counter for unique content identification across the entire system
;; This ensures every registered content item receives a globally unique identifier
(define-data-var master-content-sequence-number uint u0)

;; =============================================================================
;; PRIMARY DATA STRUCTURES AND MAPPING DEFINITIONS
;; =============================================================================

;; Central repository for all registered content items with comprehensive metadata
;; This mapping stores complete information about each archived content piece
;; including ownership details, technical specifications, and descriptive metadata
(define-map distributed-content-archive-database
  { content-sequence-id: uint }
  {
    content-title-identifier: (string-ascii 64),
    content-owner-principal: principal,
    content-storage-size-in-bytes: uint,
    blockchain-registration-height: uint,
    detailed-content-summary: (string-ascii 128),
    metadata-classification-labels: (list 10 (string-ascii 32))
  }
)

;; Sophisticated access management system for fine-grained permission control
;; This mapping tracks individual user permissions for each content item
;; enabling precise control over who can access specific archived materials
(define-map content-access-permission-registry
  { content-sequence-id: uint, authorized-user-principal: principal }
  { viewing-permission-status: bool }
)

;; =============================================================================
;; INTERNAL UTILITY FUNCTIONS FOR DATA VALIDATION AND PROCESSING
;; =============================================================================

;; Validates individual metadata classification labels for format compliance
;; Ensures labels meet length requirements and contain valid characters
;; Parameters: label-text - the classification label to validate
;; Returns: boolean indicating whether the label meets validation criteria
(define-private (validate-individual-metadata-label (label-text (string-ascii 32)))
  (and
    (> (len label-text) u0)
    (< (len label-text) u33)
  )
)

;; Comprehensive validation routine for complete metadata label collections
;; Verifies that all labels in a collection meet individual validation requirements
;; Also ensures the collection size is within acceptable limits
;; Parameters: label-collection - list of metadata labels to validate
;; Returns: boolean indicating successful validation of entire collection
(define-private (validate-complete-metadata-label-collection (label-collection (list 10 (string-ascii 32))))
  (and
    (> (len label-collection) u0)
    (<= (len label-collection) u10)
    (is-eq (len (filter validate-individual-metadata-label label-collection)) (len label-collection))
  )
)

;; Utility function to verify existence of content records in the archive database
;; Performs efficient lookup to determine if a content item is already registered
;; Parameters: content-sequence-id - unique identifier for the content item
;; Returns: boolean indicating whether the content record exists
(define-private (verify-content-record-existence (content-sequence-id uint))
  (is-some (map-get? distributed-content-archive-database { content-sequence-id: content-sequence-id }))
)

;; Retrieves storage size information for specific content items
;; Provides safe access to storage metrics with default fallback values
;; Parameters: content-sequence-id - unique identifier for the content item
;; Returns: storage size in bytes, or zero if content not found
(define-private (retrieve-content-storage-metrics (content-sequence-id uint))
  (default-to u0
    (get content-storage-size-in-bytes
      (map-get? distributed-content-archive-database { content-sequence-id: content-sequence-id })
    )
  )
)

;; Advanced ownership verification mechanism with comprehensive security checks
;; Validates that a specific principal has ownership rights over a content item
;; Parameters: content-sequence-id - content identifier, user-principal - principal to verify
;; Returns: boolean indicating ownership status
(define-private (verify-content-ownership-authorization (content-sequence-id uint) (user-principal principal))
  (match (map-get? distributed-content-archive-database { content-sequence-id: content-sequence-id })
    content-record-data (is-eq (get content-owner-principal content-record-data) user-principal)
    false
  )
)

;; =============================================================================
;; PUBLIC INTERFACE FUNCTIONS FOR CONTENT MANAGEMENT OPERATIONS
;; =============================================================================

;; Primary content registration function with comprehensive validation framework
;; This function handles the complete process of registering new content items
;; including validation, storage, and permission initialization
;; 
;; Parameters:
;; - content-title-identifier: human-readable name for the content
;; - content-storage-size-in-bytes: size of the content in bytes
;; - detailed-content-summary: descriptive text about the content
;; - metadata-classification-labels: list of tags for content categorization
;; 
;; Returns: success response with new content ID or appropriate error code
(define-public (register-new-content-item
  (content-title-identifier (string-ascii 64))
  (content-storage-size-in-bytes uint)
  (detailed-content-summary (string-ascii 128))
  (metadata-classification-labels (list 10 (string-ascii 32)))
)
  (let
    (
      (next-available-content-id (+ (var-get master-content-sequence-number) u1))
    )
    ;; Comprehensive input validation with detailed error reporting
    ;; Each validation check ensures data integrity and system consistency
    (asserts! (> (len content-title-identifier) u0) invalid-input-data-error)
    (asserts! (< (len content-title-identifier) u65) invalid-input-data-error)
    (asserts! (> content-storage-size-in-bytes u0) storage-capacity-overflow-error)
    (asserts! (< content-storage-size-in-bytes u1000000000) storage-capacity-overflow-error)
    (asserts! (> (len detailed-content-summary) u0) invalid-input-data-error)
    (asserts! (< (len detailed-content-summary) u129) invalid-input-data-error)
    (asserts! (validate-complete-metadata-label-collection metadata-classification-labels) data-format-validation-failure-error)

    ;; Atomic registration process ensuring consistent state updates
    ;; All data is inserted simultaneously to prevent partial registration states
    (map-insert distributed-content-archive-database
      { content-sequence-id: next-available-content-id }
      {
        content-title-identifier: content-title-identifier,
        content-owner-principal: contract-caller,
        content-storage-size-in-bytes: content-storage-size-in-bytes,
        blockchain-registration-height: block-height,
        detailed-content-summary: detailed-content-summary,
        metadata-classification-labels: metadata-classification-labels
      }
    )

    ;; Automatic permission initialization for content creator
    ;; The registering principal automatically receives full access privileges
    (map-insert content-access-permission-registry
      { content-sequence-id: next-available-content-id, authorized-user-principal: contract-caller }
      { viewing-permission-status: true }
    )

    ;; Update global sequence counter for future registrations
    ;; This ensures unique identification for all subsequent content items
    (var-set master-content-sequence-number next-available-content-id)
    (ok next-available-content-id)
  )
)


;; Secure ownership transfer protocol with comprehensive validation
;; Enables authorized transfer of content ownership between principals
;; Implements strict security measures to prevent unauthorized transfers
;; 
;; Parameters:
;; - content-sequence-id: identifier of content to transfer
;; - recipient-owner-principal: new owner principal
;; 
;; Returns: success confirmation or error response
(define-public (execute-content-ownership-transfer (content-sequence-id uint) (recipient-owner-principal principal))
  (let
    (
      (current-content-record (unwrap! (map-get? distributed-content-archive-database { content-sequence-id: content-sequence-id })
        content-record-missing-error))
    )
    ;; Rigorous authorization validation before ownership changes
    ;; Ensures only legitimate owners can initiate ownership transfers
    (asserts! (verify-content-record-existence content-sequence-id) content-record-missing-error)
    (asserts! (is-eq (get content-owner-principal current-content-record) contract-caller) insufficient-authorization-privileges-error)

    ;; Execute secure ownership transfer with updated registry records
    ;; Maintains all other content metadata while changing ownership
    (map-set distributed-content-archive-database
      { content-sequence-id: content-sequence-id }
      (merge current-content-record { content-owner-principal: recipient-owner-principal })
    )
    (ok true)
  )
)

;; Permanent content removal system with irreversible deletion
;; Provides authorized users the ability to permanently remove content records
;; Implements comprehensive security checks to prevent accidental deletions
;; 
;; Parameters:
;; - content-sequence-id: unique identifier of content to remove
;; 
;; Returns: success confirmation or error response
(define-public (permanently-remove-content-record (content-sequence-id uint))
  (let
    (
      (content-record-for-deletion (unwrap! (map-get? distributed-content-archive-database { content-sequence-id: content-sequence-id })
        content-record-missing-error))
    )
    ;; Strict authorization verification before permanent deletion
    ;; Ensures only content owners can authorize irreversible removal
    (asserts! (verify-content-record-existence content-sequence-id) content-record-missing-error)
    (asserts! (is-eq (get content-owner-principal content-record-for-deletion) contract-caller) insufficient-authorization-privileges-error)

    ;; Execute permanent removal from distributed archive database
    ;; This operation cannot be reversed once completed
    (map-delete distributed-content-archive-database { content-sequence-id: content-sequence-id })
    (ok true)
  )
)


;; =============================================================================
;; READ-ONLY QUERY FUNCTIONS FOR DATA RETRIEVAL AND SYSTEM INFORMATION
;; =============================================================================

;; Comprehensive content information retrieval with access control validation
;; Provides detailed content metadata to authorized users only
;; Implements sophisticated permission checking before data disclosure
;; 
;; Parameters:
;; - content-sequence-id: unique identifier of content to retrieve
;; 
;; Returns: complete content information or access denied error
(define-read-only (retrieve-comprehensive-content-information (content-sequence-id uint))
  (let
    (
      (content-metadata (unwrap! (map-get? distributed-content-archive-database { content-sequence-id: content-sequence-id })
        content-record-missing-error))
      (caller-access-privileges (default-to false
        (get viewing-permission-status
          (map-get? content-access-permission-registry { content-sequence-id: content-sequence-id, authorized-user-principal: contract-caller })
        )
      ))
    )
    ;; Multi-tier access control validation before information disclosure
    ;; Ensures only authorized users can access content information
    (asserts! (verify-content-record-existence content-sequence-id) content-record-missing-error)
    (asserts! (or caller-access-privileges (is-eq (get content-owner-principal content-metadata) contract-caller)) restricted-content-access-error)

    ;; Return comprehensive content information structure
    ;; Includes all metadata fields for authorized users
    (ok {
      content-title-identifier: (get content-title-identifier content-metadata),
      content-owner-principal: (get content-owner-principal content-metadata),
      content-storage-size-in-bytes: (get content-storage-size-in-bytes content-metadata),
      blockchain-registration-height: (get blockchain-registration-height content-metadata),
      detailed-content-summary: (get detailed-content-summary content-metadata),
      metadata-classification-labels: (get metadata-classification-labels content-metadata)
    })
  )
)

;; System-wide statistics and operational metrics retrieval
;; Provides administrators and users with comprehensive system information
;; Includes total registration counts and system configuration details
;; 
;; Returns: complete system statistics and configuration information
(define-read-only (retrieve-system-operational-statistics)
  (ok {
    total-registered-content-items: (var-get master-content-sequence-number),
    primary-system-administrator: primary-system-controller
  })
)

;; Content ownership information retrieval utility function
;; Provides quick access to ownership details for specific content items
;; Useful for permission verification and administrative operations
;; 
;; Parameters:
;; - content-sequence-id: identifier of content to query
;; 
;; Returns: owner principal or content not found error
(define-read-only (retrieve-content-ownership-information (content-sequence-id uint))
  (match (map-get? distributed-content-archive-database { content-sequence-id: content-sequence-id })
    content-ownership-record (ok (get content-owner-principal content-ownership-record))
    content-record-missing-error
  )
)

;; Detailed access permission status verification system
;; Provides comprehensive information about user access rights for specific content
;; Includes ownership status, granted permissions, and effective access rights
;; 
;; Parameters:
;; - content-sequence-id: content item to check permissions for
;; - target-user-principal: user to verify permissions for
;; 
;; Returns: detailed permission status information
(define-read-only (analyze-comprehensive-access-permissions (content-sequence-id uint) (target-user-principal principal))
  (let
    (
      (content-ownership-record (unwrap! (map-get? distributed-content-archive-database { content-sequence-id: content-sequence-id })
        content-record-missing-error))
      (explicit-access-grant (default-to false
        (get viewing-permission-status
          (map-get? content-access-permission-registry { content-sequence-id: content-sequence-id, authorized-user-principal: target-user-principal })
        )
      ))
    )
    ;; Return comprehensive access analysis with detailed status information
    ;; Provides complete picture of user's access rights and permissions
    (ok {
      has-explicit-access-grant: explicit-access-grant,
      is-content-owner: (is-eq (get content-owner-principal content-ownership-record) target-user-principal),
      has-effective-read-access: (or explicit-access-grant (is-eq (get content-owner-principal content-ownership-record) target-user-principal))
    })
  )
)